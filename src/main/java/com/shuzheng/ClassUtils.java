package com.shuzheng;

import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;

import java.util.*;

public class ClassUtils {


    /**
     * 查找某个类的实现类
     *
     * @param clazz could interface or abstract class
     * @return return clazz implement
     * @throws Exception
     */
    public static InstanceUtil findUseImplementsByClass(final Class<?> clazz, final String... filterPkg) throws Exception {
        InstanceUtil instanceUtil = new InstanceUtil(clazz);
        instanceUtil.findAndFilterInstance(filterPkg);
        return instanceUtil;
    }

    private static void check(final InstanceUtil instanceUtil) throws InstantiationException {
        if (null == instanceUtil) {
            throw new InstantiationException("instanceUtil is null");
        }
    }

    public static Set<String> getInstances(final InstanceUtil instanceUtil) throws InstantiationException {
        check(instanceUtil);
        return instanceUtil.getAllClassInstances();
    }

    public enum InstanceType {
        Method,
        Field,
        ;
    }

    /**
     * search clazz's implements class
     *
     * @param clazz want to search clazz
     * @return all the implements class of clazz
     */
    public static Set<Class<?>> scannerPackages(final Class<?> clazz) {
        Reflections reflections = new Reflections("com.zaxxer", new SubTypesScanner());
        return new HashSet<>(reflections.getSubTypesOf(clazz));
    }
}
