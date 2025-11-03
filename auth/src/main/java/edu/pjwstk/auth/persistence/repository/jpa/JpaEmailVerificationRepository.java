package edu.pjwstk.auth.persistence.repository.jpa;

import edu.pjwstk.auth.persistence.entity.EmailVerificationEntity;
import jakarta.transaction.Transactional;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface JpaEmailVerificationRepository extends JpaRepository<EmailVerificationEntity, UUID> {

    Optional<EmailVerificationEntity> findByUserIdAndCode(UUID id, String code);

    List<EmailVerificationEntity> findByUserIdAndRevoked(UUID userId, boolean revoked, Sort sort);

    @Modifying
    @Transactional
    @Query("""
                UPDATE EmailVerificationEntity ev
                SET ev.revoked = true
                WHERE ev.userId = :userId
                    AND ev.revoked = false
                    AND ev.expiresAt > CURRENT_TIMESTAMP
            """)
    void revokeAllActiveEmailVerificationCodesByUserId(@Param("userId") UUID userId);
}
