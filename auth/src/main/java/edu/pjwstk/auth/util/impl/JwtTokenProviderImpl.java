package edu.pjwstk.auth.util.impl;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.auth.util.TokenProvider;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import org.apache.commons.codec.digest.DigestUtils;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Date;
import java.util.UUID;

public class JwtTokenProviderImpl implements TokenProvider {

    private final SecretKey secretKey;
    private final long accessTokenExpirationTime;
    private final long refreshTokenExpirationTime;

    public JwtTokenProviderImpl(
            String secretKey,
            long accessTokenExpirationTime,
            long refreshTokenExpirationTime
    ) {
        this.secretKey = new SecretKeySpec(
                secretKey.getBytes(StandardCharsets.UTF_8),
                "HmacSHA256"
        );
        this.accessTokenExpirationTime = accessTokenExpirationTime;
        this.refreshTokenExpirationTime = refreshTokenExpirationTime;
    }

    @Override
    public String generateAccessToken(UUID userId, String email) {
        return Jwts.builder()
                .subject(email)
                .claim("userId", userId)
                .issuedAt(Date.from(Instant.now()))
                .expiration(Date.from(Instant.now().plusSeconds(accessTokenExpirationTime)))
                .signWith(secretKey)
                .compact();
    }

    @Override
    public Claims validateTokenAndExtractClaims(String token) {
        return Jwts.parser()
                .verifyWith(secretKey)
                .build()
                .parseSignedClaims(token)
                .getPayload();
    }

    @Override
    public AuthTokens generateTokenPair(UUID userId, String email, boolean isEmailVerified) {
        return new AuthTokens(
                generateAccessToken(userId, email),
                UUID.randomUUID().toString()
        );
    }

    @Override
    public String hashToken(String token) {
        return DigestUtils.sha256Hex(token);
    }

    @Override
    public long getRefreshTokenExpirationTime() {
        return refreshTokenExpirationTime;
    }

}
