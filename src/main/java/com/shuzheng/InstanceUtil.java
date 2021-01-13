package com.shuzheng;

import com.sun.jdi.*;
import com.sun.jdi.connect.AttachingConnector;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;
import com.sun.tools.attach.VirtualMachine;
import sun.jvm.hotspot.jdi.SAPIDAttachingConnector;
import sun.tools.attach.HotSpotVirtualMachine;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.net.ConnectException;
import java.util.*;

/**
 * @author wangshuzheng
 * @date 2020/11/30 5:37 下午
 * @description cp util.
 */
public class InstanceUtil {

    private final Class<?> classType;
    private final String clazzName;

    private final Set<String> dataSourceClasses;

    private ReferenceType referenceType;
    private List<ObjectReference> objectReferences;
    private Set<String> classInstances;

    private com.sun.jdi.VirtualMachine virtualMachine;
    private VirtualMachine machine;
    private String port = "";
    private String host = "localhost";

    private boolean canReadInfo = false;
    private final String pid;

    public InstanceUtil(final Class<?> clazz) throws Exception {
        classType = clazz;
        clazzName = classType.getName();
        this.dataSourceClasses = new TreeSet<>();
        pid = getPid();
        this.machine = VirtualMachine.attach(/*String.valueOf(Integer.parseInt(pid) - 1)*/pid);
    }

    public boolean isCanReadInfo() {
        return canReadInfo;
    }

    public Set<String> getAllClassInstances() {
        return classInstances;
    }

    /**
     * @param filterPkg the implements class is not in filterPkg
     * @throws Exception
     */
    public void findAndFilterInstance(final String... filterPkg) throws Exception {
        final Set<Class<?>> classes = ClassUtils.scannerPackages(classType);
        if (!classes.isEmpty()) {
            for (Class<?> aClass : classes) {
                boolean isFilterPkg = false;
                if (null != filterPkg) {
                    for (String fpkg : filterPkg) {
                        if (aClass.getName().startsWith(fpkg)) {
                            isFilterPkg = true;
                            break;
                        }
                    }
                }
                if (!isFilterPkg) {
                    dataSourceClasses.add(aClass.getName());
                }
            }
            if (!dataSourceClasses.isEmpty()) {
                Thread thread = new Thread(new InstanceRunner());
                // TODO need comment when test, otherwise will can't get
                thread.setDaemon(true);
                thread.setName(clazzName + "-search-thread-1");
                thread.start();
            }
        }
    }

    private class InstanceRunner implements Runnable {
        private HotSpotVirtualMachine saJdiVirtualMachine;

        public InstanceRunner() throws Exception {
            if (machine instanceof HotSpotVirtualMachine) {
                this.saJdiVirtualMachine = (HotSpotVirtualMachine) machine;
            } else {
                throw new ClassNotFoundException("HotSpotVirtualMachine not found!");
            }
        }

        @Override
        public void run() {
            while (true) {
                try {
                    classInstances = getContainInstance();
                    if (null != classInstances && !classInstances.isEmpty()) {
                        processPort();
                        canReadInfo = true;
                        break;
                    }
                    Thread.sleep(10000, new Random().nextInt(500));
                } catch (Exception e) {
                    e.printStackTrace();
                    if (e instanceof ConnectException) {
                        break;
                    }
                }
            }
        }

        private void processPort() throws Exception {
            Properties properties = saJdiVirtualMachine.getAgentProperties();
            String portInfo = properties.getProperty("sun.jvm.args");
            String[] portInfoSplit = portInfo.split("=[\\d]+:");
            if (portInfoSplit.length >= 2) {
                String inner = portInfo.substring(0, portInfo.length() - portInfoSplit[1].length());
                port = inner.substring(portInfoSplit[0].length() + 1, inner.length() - 1);
            }
            if (null == port || port.isEmpty()) {
                portInfoSplit = portInfo.split("[\\d]+:");
                if (portInfoSplit.length >= 2) {
                    port = portInfoSplit[1].split(",")[0];
                }
            }
            port = "56856";
        }

        private Set<String> getContainInstance() throws Exception {
            if (null != saJdiVirtualMachine) {
                final Set<String> classInstances = new HashSet<>();
                try {
                    InputStream is = saJdiVirtualMachine.heapHisto("-all");
                    BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(is));
                    String line;
                    String[] fileds;
                    while ((line = bufferedReader.readLine()) != null) {
                        fileds = line.split("\\s+");
                        if (fileds.length >= 5) {
                            if (dataSourceClasses.contains(fileds[4])) {
                                classInstances.add(fileds[4]);
                            }
                        }
                    }
                    return classInstances;
                } catch (IOException e) {
                    if (e.getMessage().equalsIgnoreCase("Detached from target VM")) {
                        machine = VirtualMachine.attach(pid);
                        if (machine instanceof HotSpotVirtualMachine) {
                            this.saJdiVirtualMachine = (HotSpotVirtualMachine) machine;
                        } else {
                            throw new Exception(e);
                        }
                    }
                }
            }
            return null;
        }
    }

    private synchronized void buildVirtualMachineBySA() throws Exception {
        AttachingConnector connector = new SAPIDAttachingConnector();
        //set the arguments for the connector
        Map<String, Connector.Argument> arg = connector.defaultArguments();
//            arg.get("hostname").setValue(host);
//            arg.get("port").setValue(port);
        arg.get("pid").setValue(pid);
        virtualMachine = connector.attach(arg);
    }

    private synchronized void attach() throws IOException, IllegalConnectorArgumentsException {
        //getSocketAttaching connector to connect to other JVM using Socket
        AttachingConnector connector = Bootstrap.virtualMachineManager().attachingConnectors()
                .stream().filter(p -> p.transport().name().contains("socket"))
                .findFirst().get();
        //set the arguments for the connector
        Map<String, Connector.Argument> arg = connector.defaultArguments();
        arg.get("hostname").setValue(host);
        if (null == port || port.isEmpty()) {
            throw new NullPointerException("port is null");
        }
        arg.get("port").setValue(port);
        //connect to remote process by socket
        virtualMachine = connector.attach(arg);
    }

    /**
     * @param className
     * @throws Exception
     */
    private void initReference(final String className) throws Exception {
        //get all classes of java.lang.String. There would be only one element.
        if (null != className) {
            List<ReferenceType> classes = virtualMachine.classesByName(className);
            if (null == classes || classes.isEmpty()) {
                throw new Exception("not found className: " + className + "'s instance");
            }
            referenceType = classes.get(0);
            //get all instances of a classes (set maximum count of instannces to get).
            objectReferences = classes.get(0).instances(100000);
            //objectReference holds referenct to remote object.
        }
    }

    public Map<String, Object> getResultByInstance(final String className, ClassUtils.InstanceType type, final String... names) throws Exception {
        if (null == className || className.isEmpty()) {
            throw new NullPointerException("className is null");
        }
        if (null == type) {
            type = ClassUtils.InstanceType.Method;
        }
        try {
            attach();
//            buildVirtualMachineBySA();
            System.out.println(virtualMachine);
            initReference(className);
            switch (type) {
                case Method:
                    return getInstanceValueByMethod(names);
                case Field:
                    return getInstanceValueByFields(names);
                default:
                    throw new UnsupportedOperationException("please set a InstanceType");
            }
        } finally {
            try {
                machine.detach();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * get field's value by fields
     *
     * @param methodNames want to search the special methodNames
     * @return invoke the result of the special method
     */
    private Map<String, Object> getInstanceValueByMethod(String... methodNames) {
        if (null != methodNames && methodNames.length > 0) {
            Map<String, Object> objects = new HashMap<>();
            Arrays.stream(methodNames).forEach(mn -> {
                ThreadReference threadReference = null;
                List<Method> methods = referenceType.methodsByName(mn);
                if (null != methods && !methods.isEmpty()) {
                    if (null != virtualMachine.allThreads() && !virtualMachine.allThreads().isEmpty()) {
                        for (ThreadReference tr : virtualMachine.allThreads()) {
                            // datasource-search-thread-1
                            if (tr.name().equalsIgnoreCase("main")) {
                                threadReference = tr;
                            }
                        }
                    }
                    if (null != threadReference) {
                        Method method = methods.get(0);
                        for (ObjectReference objectReference : objectReferences) {
                            //show text representation of remote object
                            try {
                                objects.put(mn, objectReference.invokeMethod(threadReference, method, new ArrayList<>(), 0));
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        }
                    }
                }
            });
            return objects;
        }
        return null;
    }

    /**
     * get field's value by fields
     *
     * @param fieldNames want to search field's name
     * @return the special field value
     */
    private Map<String, Object> getInstanceValueByFields(String... fieldNames) {
        if (null != fieldNames && fieldNames.length > 0) {
            Map<String, Object> objects = new HashMap<>();
            Arrays.stream(fieldNames).forEach(fn -> {
                Field field = referenceType.fieldByName(fn);
                System.out.println(objectReferences);
                for (ObjectReference objectReference : objectReferences) {
                    //show text representation of remote object
                    objects.put(fn, objectReference.getValue(field));
                }
            });
            return objects;
        }
        return null;
    }

    private String getPid() {
        RuntimeMXBean bean = ManagementFactory.getRuntimeMXBean();
        String name = bean.getName();
        int index = name.indexOf("@");
        return name.substring(0, index);
    }

}
