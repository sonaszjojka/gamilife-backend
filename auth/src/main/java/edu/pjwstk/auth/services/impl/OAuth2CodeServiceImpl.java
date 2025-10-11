package edu.pjwstk.auth.services.impl;

import com.auth0.jwt.JWT;
import com.auth0.jwt.interfaces.DecodedJWT;
import edu.pjwstk.auth.dto.service.GoogleUserDto;
import edu.pjwstk.auth.services.OAuth2CodeService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.Map;

@Service
public class OAuth2CodeServiceImpl implements OAuth2CodeService {

    private final WebClient webClient;

    @Value("${spring.security.oauth2.client.registration.google.client-id}")
    private String googleClientId;

    @Value("${spring.security.oauth2.client.registration.google.client-secret}")
    private String googleClientSecret;

    public OAuth2CodeServiceImpl(WebClient webClient) {
        this.webClient = webClient;
    }

    @Override
    public Map<String, String> exchangeCodeForTokens(String code, String codeVerifier) {
        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        formData.add("grant_type", "authorization_code");
        formData.add("code", code);
        formData.add("client_id", googleClientId);
        formData.add("client_secret", googleClientSecret);
        formData.add("redirect_uri", "http://localhost:4200/oauth2/callback");
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

    @Override
    public GoogleUserDto extractUserInfoFromIdToken(String idToken) {
        DecodedJWT jwt = JWT.decode(idToken);

        return new GoogleUserDto(
                jwt.getClaim("sub").asString(),
                jwt.getClaim("email").asString(),
                jwt.getClaim("given_name").asString(),
                jwt.getClaim("family_name").asString()
        );
    }

}
