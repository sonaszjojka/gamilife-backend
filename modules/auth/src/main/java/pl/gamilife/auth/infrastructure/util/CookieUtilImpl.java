package pl.gamilife.auth.infrastructure.util;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseCookie;
import org.springframework.stereotype.Component;
import pl.gamilife.shared.web.util.CookieUtil;

import java.time.Duration;

@Component
public class CookieUtilImpl implements CookieUtil {
    @Value("${spring.tokens.refresh-token.expires-in}")
    private long refreshTokenExpiry;

    @Value("${spring.tokens.access-token.expires-in}")
    private long accessTokenExpiry;

    @Value("${server.ssl.secure}")
    private boolean secure;


    @Override
    public ResponseCookie createAccessTokenCookie(String token) {
        return createSecureCookie("ACCESS-TOKEN", token, accessTokenExpiry);
    }

    @Override
    public ResponseCookie createRefreshTokenCookie(String token) {
        return createSecureCookie("REFRESH-TOKEN", token, refreshTokenExpiry);
    }

    @Override
    public ResponseCookie invalidateAccessTokenCookie() {
        return createSecureCookie("ACCESS-TOKEN", "", 0);
    }

    @Override
    public ResponseCookie invalidateRefreshTokenCookie() {
        return createSecureCookie("REFRESH-TOKEN", "", 0);
    }

    private ResponseCookie createSecureCookie(String name, String value, long expiry) {
        return ResponseCookie.from(name, value)
                .path("/")
                .httpOnly(true)
                .maxAge(Duration.ofSeconds(expiry))
                .secure(secure)
                .sameSite("Lax")
                .build();
    }
}
