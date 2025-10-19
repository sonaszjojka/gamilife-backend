package edu.pjwstk.auth.persistence.repository;

import edu.pjwstk.auth.domain.RefreshToken;

import java.util.Optional;
import java.util.UUID;

public interface RefreshTokenRepository {
    void save(RefreshToken refreshToken);

    Optional<RefreshToken> getRefreshTokenByHashedToken(String hashedRefreshToken);

    void updateRevokedStatus(UUID id, boolean revoked);
}
