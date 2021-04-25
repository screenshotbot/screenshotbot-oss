package com.atlassian.oauth.client.example;


import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

public class PropertiesClient {
    public static final String CONSUMER_KEY = "consumer_key";
    public static final String PRIVATE_KEY = "private_key";
    public static final String REQUEST_TOKEN = "request_token";
    public static final String ACCESS_TOKEN = "access_token";
    public static final String SECRET = "secret";
    public static final String JIRA_HOME = "jira_home";


    private final static Map<String, String> DEFAULT_PROPERTY_VALUES = ImmutableMap.<String, String>builder()
            .put(JIRA_HOME, "https://screenshotbot.atlassian.net")
            .put(CONSUMER_KEY, "OauthKey")
            .put(PRIVATE_KEY, "MIICeAIBADANBgkqhkiG9w0BAQEFAASCAmIwggJeAgEAAoGBAOm1Zh5J+Cv4TQkB4qf6ZydqCsUjQ9+l0+w6PjjXX+S4D3Pz3I/sMJOUKCvZzWD2argqOZ1fP1lyjdqDORhOHhw2w/efZKYoBBPdLSwwkeG3D1/ojvMwQO0++dnWQI/qw/CYt7RCZFAM/K56Mb85gFXWzMjRDLIDJoK72laso7EpAgMBAAECgYEA2OfEDE6Ip35/OOHoFBEiFgGkV00gnHyS0MdJooW5+VQe+3YUefszJnU2R2SQ6fIqIB0IdhfFeuYH5AswPW+bHrwZr11hABZY9OTmgTvaypSEgscM3pTfK3A33EZV4s33xFr3lHnowhPvNnH2NkTpLybTiLktAb/KvVFSYVkuabUCQQD9xTk662Obp0IjEEMNu35ZF6nzhiL3IwQkzCoN20ZpyqQEESj7/F5Jw0f7h3zjcpIbL6fUPMJTZthDqxo4qZsHAkEA68MNeKUXtJtdefkxvH33ywvtt5ZmGF+atjj20n3rrYAfUgV5zEypvAwIO95ke0l9yFrp7NL863lBqWqDo2PWTwJBANE/iaQqqhSTwvYUl6XAHzupRoq15Sjw557L9jU8xH/BBqMP30KAYuLhsf4WRwVo2E6waeuYOgf5OoP5lwJ334sCQDl+UZSwPf2vPbZ3DKf42CrN9+SjarZa6jGx5o8iYgznh7vK1g76SxIupPtkmoBuRZxMf2tfcIpEAb082mlH+YUCQQDtvrsVv+ptrGAkMwiG6wbP1bhqaWMH1pf3YGOBKIV6gOPn0Wm96FUjR18Xj10VVsdVtHhB9tFKCvCGGu7+17bN")
            .build();

    private final String fileUrl;
    private final String propFileName = "config.properties";

    public PropertiesClient() throws Exception {
        fileUrl = "./" + propFileName;
    }

    public Map<String, String> getPropertiesOrDefaults() {
        try {
            Map<String, String> map = toMap(tryGetProperties());
            map.putAll(Maps.difference(map, DEFAULT_PROPERTY_VALUES).entriesOnlyOnRight());
            return map;
        } catch (FileNotFoundException e) {
            tryCreateDefaultFile();
            return new HashMap<>(DEFAULT_PROPERTY_VALUES);
        } catch (IOException e) {
            return new HashMap<>(DEFAULT_PROPERTY_VALUES);
        }
    }

    private Map<String, String> toMap(Properties properties) {
        return properties.entrySet().stream()
                .filter(entry -> entry.getValue() != null)
                .collect(Collectors.toMap(o -> o.getKey().toString(), t -> t.getValue().toString()));
    }

    private Properties toProperties(Map<String, String> propertiesMap) {
        Properties properties = new Properties();
        propertiesMap.entrySet()
                .stream()
                .forEach(entry -> properties.put(entry.getKey(), entry.getValue()));
        return properties;
    }

    private Properties tryGetProperties() throws IOException {
        InputStream inputStream = new FileInputStream(new File(fileUrl));
        Properties prop = new Properties();
        prop.load(inputStream);
        return prop;
    }

    public void savePropertiesToFile(Map<String, String> properties) {
        OutputStream outputStream = null;
        File file = new File(fileUrl);

        try {
            outputStream = new FileOutputStream(file);
            Properties p = toProperties(properties);
            p.store(outputStream, null);
        } catch (Exception e) {
            System.out.println("Exception: " + e);
        } finally {
            closeQuietly(outputStream);
        }
    }

    public void tryCreateDefaultFile() {
        System.out.println("Creating default properties file: " + propFileName);
        tryCreateFile().ifPresent(file -> savePropertiesToFile(DEFAULT_PROPERTY_VALUES));
    }

    private Optional<File> tryCreateFile() {
        try {
            File file = new File(fileUrl);
            file.createNewFile();
            return Optional.of(file);
        } catch (IOException e) {
            return Optional.empty();
        }
    }

    private void closeQuietly(Closeable closeable) {
        try {
            if (closeable != null) {
                closeable.close();
            }
        } catch (IOException e) {
            // ignored
        }
    }
}
