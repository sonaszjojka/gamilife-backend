package edu.pjwstk.auth.persistence.mapper;

import edu.pjwstk.auth.domain.EmailVerification;
import edu.pjwstk.auth.persistence.entity.EmailVerificationEntity;

public class EmailVerificationMapper {
    public static EmailVerificationEntity toEntity(EmailVerification emailVerification) {
        return new EmailVerificationEntity(
                emailVerification.id(),
                emailVerification.userId(),
                emailVerification.code(),
                emailVerification.issuedAt(),
                emailVerification.expiresAt(),
                emailVerification.revoked()
        );
    }

    public static EmailVerification toDomain(EmailVerificationEntity emailVerificationEntity) {
        return new EmailVerification(
                emailVerificationEntity.getId(),
                emailVerificationEntity.getUserId(),
                emailVerificationEntity.getCode(),
                emailVerificationEntity.getIssuedAt(),
                emailVerificationEntity.getExpiresAt(),
                emailVerificationEntity.isRevoked()
        );
    }
}
