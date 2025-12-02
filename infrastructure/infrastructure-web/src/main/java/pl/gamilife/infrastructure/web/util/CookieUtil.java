package pl.gamilife.infrastructure.web.util;

import org.springframework.http.ResponseCookie;

public interface CookieUtil {
    ResponseCookie createRefreshTokenCookie(String token);

    ResponseCookie invalidateAccessTokenCookie();

    ResponseCookie createAccessTokenCookie(String token);

    ResponseCookie invalidateRefreshTokenCookie();
}
