package pl.gamilife.auth.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.auth.domain.model.RefreshToken;
import pl.gamilife.auth.domain.port.repository.RefreshTokenRepository;
import pl.gamilife.auth.infrastructure.persistence.jpa.JpaRefreshTokenRepository;

import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class RefreshTokenRepositoryAdapter implements RefreshTokenRepository {

    private final JpaRefreshTokenRepository jpaRefreshTokenRepository;

    @Override
    public Optional<RefreshToken> findByToken(String token) {
        return jpaRefreshTokenRepository.findByToken(token);
    }

    @Override
    public void updateRevokedById(UUID refId, boolean revoked) {
        jpaRefreshTokenRepository.updateRevokedById(refId, revoked);
    }

    @Override
    public void revokeAllActiveRefreshTokensByUserId(UUID userId) {
        jpaRefreshTokenRepository.revokeAllActiveRefreshTokensByUserId(userId);
    }

    @Override
    public void save(RefreshToken refreshToken) {
        jpaRefreshTokenRepository.save(refreshToken);
    }
}
