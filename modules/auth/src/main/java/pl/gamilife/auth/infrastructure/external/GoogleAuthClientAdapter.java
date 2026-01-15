package pl.gamilife.auth.infrastructure.external;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import io.jsonwebtoken.security.Jwk;
import io.jsonwebtoken.security.JwkSet;
import io.jsonwebtoken.security.Jwks;
import io.jsonwebtoken.security.PublicJwk;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;
import pl.gamilife.auth.application.port.GoogleAuthClient;

import java.io.IOException;
import java.security.Key;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

@Slf4j
@Component
@RequiredArgsConstructor
public class GoogleAuthClientAdapter implements GoogleAuthClient {

    private final WebClient webClient;

    @Value("${spring.security.oauth2.client.registration.google.client-id}")
    private String googleClientId;

    @Value("${spring.security.oauth2.client.registration.google.client-secret}")
    private String googleClientSecret;

    @Value("${spring.security.oauth2.client.registration.google.redirect-uri}")
    private String googleRedirectUri;

    private final Cache<String, Key> keyCache = Caffeine.newBuilder()
            .expireAfterWrite(1, TimeUnit.HOURS)
            .maximumSize(10)
            .build();

    private final ReentrantLock lock = new ReentrantLock();

    @Override
    public Map<String, String> call(String code, String codeVerifier) {
        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        formData.add("grant_type", "authorization_code");
        formData.add("code", code);
        formData.add("client_id", googleClientId);
        formData.add("client_secret", googleClientSecret);
        formData.add("redirect_uri", googleRedirectUri);
        formData.add("code_verifier", codeVerifier);

        return webClient.post()
                .uri("https://oauth2.googleapis.com/token")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_FORM_URLENCODED_VALUE)
                .body(BodyInserters.fromFormData(formData))
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<Map<String, String>>() {
                })
                .block();
    }

    public Key getKey(String keyId) {
        Key key = keyCache.getIfPresent(keyId);
        if (key != null) {
            return key;
        }

        lock.lock();
        try {
            key = keyCache.getIfPresent(keyId);
            if (key != null) {
                return key;
            }

            refreshKeys();

            return keyCache.getIfPresent(keyId);
        } finally {
            lock.unlock();
        }

    }

    private void refreshKeys() {
        try {
            log.debug("Attempting to refresh Google Public Keys");
            String jsonResponse = webClient.get()
                    .uri("https://www.googleapis.com/oauth2/v3/certs")
                    .retrieve()
                    .bodyToMono(String.class)
                    .block();

            if (jsonResponse != null) {
                JwkSet jwkSet = Jwks.setParser().build().parse(jsonResponse);

                for (Jwk<?> jwk : jwkSet.getKeys()) {
                    if (jwk instanceof PublicJwk) {
                        keyCache.put(jwk.getId(), ((PublicJwk<?>) jwk).toKey());
                    }
                }
            } else {
                throw new IOException("Failed to refresh Google Public Keys");
            }

            log.debug("Google Public Keys refreshed");
        } catch (Exception e) {
            log.error("Error refreshing Google Public Keys", e);
        }
    }
}
