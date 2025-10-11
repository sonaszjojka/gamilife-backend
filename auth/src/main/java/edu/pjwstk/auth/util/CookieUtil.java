package edu.pjwstk.auth.util;

import jakarta.servlet.http.Cookie;

public interface CookieUtil {
    Cookie createRefreshTokenCookie(String token);

    Cookie invalidateAccessTokenCookie();

    Cookie createAccessTokenCookie(String token);

    Cookie invalidateRefreshTokenCookie();
}
