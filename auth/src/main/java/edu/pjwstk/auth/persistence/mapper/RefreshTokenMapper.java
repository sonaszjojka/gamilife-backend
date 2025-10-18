package edu.pjwstk.auth.persistence.mapper;

import edu.pjwstk.auth.domain.RefreshToken;
import edu.pjwstk.auth.persistence.entity.RefreshTokenEntity;

public class RefreshTokenMapper {
    public static RefreshTokenEntity toEntity(RefreshToken refreshToken) {
        return new RefreshTokenEntity(
                refreshToken.id(),
                refreshToken.userId(),
                refreshToken.token(),
                refreshToken.issuedAt(),
                refreshToken.expiresAt(),
                refreshToken.revoked()
        );
    }

    public static RefreshToken toDomain(RefreshTokenEntity refreshTokenEntity) {
        return new RefreshToken(
                refreshTokenEntity.getId(),
                refreshTokenEntity.getUserId(),
                refreshTokenEntity.getToken(),
                refreshTokenEntity.getIssuedAt(),
                refreshTokenEntity.getExpiresAt(),
                refreshTokenEntity.isRevoked()
        );
    }
}
