package com.shuzheng.test;

import com.shuzheng.ClassUtils;
import com.shuzheng.InstanceUtil;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import javax.sql.DataSource;
import java.util.*;

public class ClassUtilsTest {

    private static DataSource dataSource;

    public static void main(String[] args) {
        try {
            buildHikariDataSource();
            InstanceUtil instanceUtil = ClassUtils.findUseImplementsByClass(DataSource.class, "com.zaxxer.hikari.util");
            Set<String> strings;
            int maxTimes = 100, times = 0;
            lookForInstance:
            while (true) {
                strings = ClassUtils.getInstances(instanceUtil);
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
                Map<String, Object> info = instanceUtil.getResultByInstance(instances.get(0), ClassUtils.InstanceType.Field, "maxPoolSize", "connectionInitSql");
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
