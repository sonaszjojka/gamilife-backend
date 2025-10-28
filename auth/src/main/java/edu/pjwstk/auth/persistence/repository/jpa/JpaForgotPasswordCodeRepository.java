package edu.pjwstk.auth.persistence.repository.jpa;

import edu.pjwstk.auth.persistence.entity.ForgotPasswordCodeEntity;
import jakarta.transaction.Transactional;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface JpaForgotPasswordCodeRepository extends JpaRepository<ForgotPasswordCodeEntity, UUID> {

    Optional<ForgotPasswordCodeEntity> findByCodeAndRevokedAndExpiresAtIsGreaterThan(String code, boolean revoked, LocalDateTime expiresAtIsGreaterThan);

    List<ForgotPasswordCodeEntity> findByUserIdAndRevoked(UUID userId, boolean revoked, Sort sort);

    @Modifying
    @Transactional
    @Query("""
                UPDATE ForgotPasswordCodeEntity fpc
                SET fpc.revoked = true
                WHERE fpc.userId = :userId
                    AND fpc.revoked = false
                    AND fpc.expiresAt > CURRENT_TIMESTAMP
            """)
    void revokeAllActiveForgotPasswordCodesByUserId(@Param("userId") UUID userId);
}