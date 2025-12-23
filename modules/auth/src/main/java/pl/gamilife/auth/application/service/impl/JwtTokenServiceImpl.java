package pl.gamilife.auth.application.service.impl;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import org.apache.commons.codec.digest.DigestUtils;
import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.auth.application.dto.TokenClaims;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.model.RefreshToken;
import pl.gamilife.auth.domain.port.repository.RefreshTokenRepository;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Date;
import java.util.UUID;

public class JwtTokenServiceImpl implements TokenService {

    private final RefreshTokenRepository refreshTokenRepository;
    private final SecretKey secretKey;
    private final long accessTokenExpirationTime;
    private final long refreshTokenExpirationTime;

    public JwtTokenServiceImpl(
            RefreshTokenRepository refreshTokenRepository,
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
    public String generateAccessToken(UUID userId, String email, boolean isEmailVerified) {
        return Jwts.builder()
                .subject(email)
                .claim("userId", userId)
                .claim("isEmailVerified", isEmailVerified)
                .issuedAt(Date.from(Instant.now()))
                .expiration(Date.from(Instant.now().plusSeconds(accessTokenExpirationTime)))
                .signWith(secretKey)
                .compact();
    }

    @Override
    public TokenClaims validateTokenAndExtractClaims(String token) {
        Claims payload = Jwts.parser()
                .verifyWith(secretKey)
                .build()
                .parseSignedClaims(token)
                .getPayload();
        return new TokenClaims(
                UUID.fromString(payload.get("userId", String.class)),
                payload.getSubject(),
                payload.get("isEmailVerified", Boolean.class)
        );
    }

    @Override
    public AuthTokens generateTokenPair(UUID userId, String email, boolean isEmailVerified) {
        AuthTokens authTokens = new AuthTokens(
                generateAccessToken(userId, email, isEmailVerified),
                generateRefreshToken()
        );

        refreshTokenRepository.save(RefreshToken.create(
                userId,
                hashToken(authTokens.refreshToken()),
                refreshTokenExpirationTime
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
