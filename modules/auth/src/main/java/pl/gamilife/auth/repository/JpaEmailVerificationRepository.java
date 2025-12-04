package pl.gamilife.auth.repository;

import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pl.gamilife.auth.models.EmailVerificationCode;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface JpaEmailVerificationRepository extends JpaRepository<EmailVerificationCode, UUID> {

    Optional<EmailVerificationCode> findByUserIdAndCode(UUID id, String code);

    List<EmailVerificationCode> findByUserIdAndRevokedOrderByIssuedAtDesc(UUID userId, boolean revoked);

    @Modifying
    @Transactional
    @Query("""
                UPDATE EmailVerificationCode ev
                SET ev.revoked = true
                WHERE ev.userId = :userId
                    AND ev.revoked = false
                    AND ev.expiresAt > CURRENT_TIMESTAMP
            """)
    void revokeAllActiveEmailVerificationCodesByUserId(@Param("userId") UUID userId);
}
