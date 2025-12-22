package pl.gamilife.auth.infrastructure.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.web.reactive.function.client.WebClient;
import pl.gamilife.auth.domain.port.repository.RefreshTokenRepository;
import pl.gamilife.auth.infrastructure.security.JwtAuthenticationFilter;
import pl.gamilife.auth.service.TokenService;
import pl.gamilife.auth.service.impl.JwtTokenServiceImpl;

@Configuration
public class AuthConfig {

    @Bean
    public JwtAuthenticationFilter jwtAuthenticationFilter(TokenService tokenService) {
        return new JwtAuthenticationFilter(tokenService);
    }

    @Bean
    public JwtTokenServiceImpl jwtTokenService(
            RefreshTokenRepository refreshTokenRepository,
            @Value("${spring.tokens.jwt.secret}") String secretKey,
            @Value("${spring.tokens.access-token.expires-in}") long accessTokenExpirationTime,
            @Value("${spring.tokens.refresh-token.expires-in}") long refreshTokenExpirationTime
    ) {
        return new JwtTokenServiceImpl(
                refreshTokenRepository,
                secretKey,
                accessTokenExpirationTime,
                refreshTokenExpirationTime
        );
    }

    @Bean
    public BCryptPasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder(12);
    }

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration authenticationConfiguration) throws Exception {
        return authenticationConfiguration.getAuthenticationManager();
    }

    @Bean
    public WebClient webClient(WebClient.Builder builder) {
        return builder.build();
    }
}
