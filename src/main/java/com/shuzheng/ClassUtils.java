package com.shuzheng;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;

import javax.sql.DataSource;
import java.util.*;

public class ClassUtils {

    private static DataSource dataSource;

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

    public static void main(String[] args) {
        try {
            buildHikariDataSource();
            InstanceUtil instanceUtil = findUseImplementsByClass(DataSource.class, "com.zaxxer.hikari.util");
            Set<String> strings;
            int maxTimes = 100, times = 0;
            lookForInstance:
            while (true) {
                strings = getInstances(instanceUtil);
                if (instanceUtil.isCanReadInfo()) {
                    break lookForInstance;
                }
                Thread.sleep(2000);
                if (times > maxTimes) {
                    break;
                }
                times++;
            }

            if (null != strings && !strings.isEmpty()) {
                List<String> instances = new ArrayList<>(strings);
                Map<String, Object> info = instanceUtil.getResultByInstance(instances.get(0), InstanceType.Field, "maxPoolSize", "connectionInitSql");
                info.forEach((key, value) -> {
                    System.out.println(key + ":" + value);
                });
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void buildHikariDataSource() {
        Properties props = new Properties();
        props.setProperty("useServerPrepStmts", Boolean.TRUE.toString());
        props.setProperty("cachePrepStmts", "true");
        props.setProperty("prepStmtCacheSize", "500");
        props.setProperty("prepStmtCacheSqlLimit", "2048");
        props.setProperty("useLocalSessionState", Boolean.TRUE.toString());
        props.setProperty("rewriteBatchedStatements", Boolean.TRUE.toString());
        props.setProperty("cacheResultSetMetadata", Boolean.TRUE.toString());
        props.setProperty("cacheServerConfiguration", Boolean.TRUE.toString());
        props.setProperty("elideSetAutoCommits", Boolean.TRUE.toString());
        props.setProperty("maintainTimeStats", Boolean.FALSE.toString());
        props.setProperty("netTimeoutForStreamingResults", "0");
        HikariConfig config = new HikariConfig();
        config.setDriverClassName("com.mysql.jdbc.Driver");
        String VITESS_JDBC_URL = "jdbc:mysql://localhost:3306/test?user=test&password=test&serverTimezone=Asia/Shanghai";
        config.setJdbcUrl(VITESS_JDBC_URL);
        config.setConnectionInitSql("select 1");
        config.setMinimumIdle(5);
        config.setMaximumPoolSize(10);
        config.setDataSourceProperties(props);
        dataSource = new HikariDataSource(config);

    }

}
