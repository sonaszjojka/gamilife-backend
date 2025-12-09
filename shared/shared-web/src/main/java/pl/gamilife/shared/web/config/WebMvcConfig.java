package pl.gamilife.shared.web.config;

import lombok.AllArgsConstructor;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import pl.gamilife.shared.web.security.CurrentUserIdArgumentResolver;
import pl.gamilife.shared.web.security.EmailVerificationInterceptor;

import java.util.List;

@Configuration
@AllArgsConstructor
public class WebMvcConfig implements WebMvcConfigurer {

    private final CurrentUserIdArgumentResolver currentUserIdArgumentResolver;
    private final EmailVerificationInterceptor emailVerificationInterceptor;
    private final SecurityProperties securityProperties;

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        resolvers.add(currentUserIdArgumentResolver);
    }

    @Override
    public void addInterceptors(org.springframework.web.servlet.config.annotation.InterceptorRegistry registry) {
        registry.addInterceptor(emailVerificationInterceptor)
                .excludePathPatterns(
                        securityProperties.getPublicPaths().toArray(new String[0])
                );
    }
}
