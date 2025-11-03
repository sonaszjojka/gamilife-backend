package edu.pjwstk.auth.persistence.repository.impl;

import edu.pjwstk.auth.domain.RefreshToken;
import edu.pjwstk.auth.persistence.entity.RefreshTokenEntity;
import edu.pjwstk.auth.persistence.mapper.RefreshTokenMapper;
import edu.pjwstk.auth.persistence.repository.RefreshTokenRepository;
import edu.pjwstk.auth.persistence.repository.jpa.JpaRefreshTokenRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class RefreshTokenRepositoryImpl implements RefreshTokenRepository {

    private final JpaRefreshTokenRepository jpaRefreshTokenRepository;

    public RefreshTokenRepositoryImpl(JpaRefreshTokenRepository jpaRefreshTokenRepository) {
        this.jpaRefreshTokenRepository = jpaRefreshTokenRepository;
    }

    @Override
    public void save(RefreshToken refreshToken) {
        RefreshTokenEntity refreshTokenEntity = RefreshTokenMapper.toEntity(refreshToken);
        jpaRefreshTokenRepository.save(refreshTokenEntity);
    }

    @Override
    public Optional<RefreshToken> getRefreshTokenByHashedToken(String hashedRefreshToken) {
        return jpaRefreshTokenRepository
                .findByToken(hashedRefreshToken)
                .map(RefreshTokenMapper::toDomain);
    }

    @Override
    public void updateRevokedStatus(UUID id, boolean revoked) {
        jpaRefreshTokenRepository.updateRevokedById(id, revoked);
    }

    @Override
    public void revokeAllActiveRefreshTokensByUserId(UUID userId) {
        jpaRefreshTokenRepository.revokeAllActiveRefreshTokensByUserId(userId);
    }
}
