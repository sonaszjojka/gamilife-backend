package pl.gamilife.shared.web.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Configuration
@ConfigurationProperties(prefix = "app.security")
public class SecurityProperties {
    private List<String> publicPaths = new ArrayList<>();

    public String[] getPublicPathsAsArray() {
        return publicPaths.toArray(new String[0]);
    }
}