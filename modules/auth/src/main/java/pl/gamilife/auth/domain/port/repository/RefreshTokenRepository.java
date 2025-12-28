package pl.gamilife.auth.domain.port.repository;

import pl.gamilife.auth.domain.model.RefreshToken;

import java.util.Optional;
import java.util.UUID;

public interface RefreshTokenRepository {

    Optional<RefreshToken> findByToken(String token);

    void updateRevokedById(UUID refId, boolean revoked);

    void revokeAllActiveRefreshTokensByUserId(UUID userId);

    void save(RefreshToken refreshToken);
}
