package edu.pjwstk.auth.persistence.mapper;

import edu.pjwstk.auth.domain.ForgotPasswordCode;
import edu.pjwstk.auth.persistence.entity.ForgotPasswordCodeEntity;

public class ForgotPasswordCodeMapper {
    public static ForgotPasswordCodeEntity toEntity(ForgotPasswordCode domain) {
        return new ForgotPasswordCodeEntity(
                domain.id(),
                domain.userId(),
                domain.code(),
                domain.issuedAt(),
                domain.expiresAt(),
                domain.revoked()
        );
    }

    public static ForgotPasswordCode toDomain(ForgotPasswordCodeEntity entity) {
        return new ForgotPasswordCode(
                entity.getId(),
                entity.getUserId(),
                entity.getCode(),
                entity.getIssuedAt(),
                entity.getExpiresAt(),
                entity.isRevoked()
        );
    }
}
