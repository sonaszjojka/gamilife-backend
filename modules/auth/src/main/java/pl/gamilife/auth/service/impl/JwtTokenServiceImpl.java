package pl.gamilife.auth.service.impl;

import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.auth.models.RefreshToken;
import pl.gamilife.auth.repository.JpaRefreshTokenRepository;
import pl.gamilife.auth.service.TokenService;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import org.apache.commons.codec.digest.DigestUtils;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.UUID;

public class JwtTokenServiceImpl implements TokenService {

    private final JpaRefreshTokenRepository refreshTokenRepository;
    private final SecretKey secretKey;
    private final long accessTokenExpirationTime;
    private final long refreshTokenExpirationTime;

    public JwtTokenServiceImpl(
            JpaRefreshTokenRepository refreshTokenRepository,
            String secretKey,
            long accessTokenExpirationTime,
            long refreshTokenExpirationTime
    ) {
        this.refreshTokenRepository = refreshTokenRepository;
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
        AuthTokens authTokens = new AuthTokens(
                generateAccessToken(userId, email),
                generateRefreshToken()
        );

        refreshTokenRepository.save(new RefreshToken(
                UUID.randomUUID(),
                userId,
                hashToken(authTokens.refreshToken()),
                LocalDateTime.now(),
                LocalDateTime.now().plusSeconds(refreshTokenExpirationTime),
                false
        ));

        return authTokens;
    }

    @Override
    public String hashToken(String token) {
        return DigestUtils.sha256Hex(token);
    }

    @Override
    public void revokeAllActiveRefreshTokensByUserId(UUID userId) {
        refreshTokenRepository.revokeAllActiveRefreshTokensByUserId(userId);
    }

    private String generateRefreshToken() {
        return UUID.randomUUID().toString();
    }
}
