package edu.pjwstk.auth.persistence.repository.jpa;

import edu.pjwstk.auth.persistence.entity.EmailVerificationEntity;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface JpaEmailVerificationRepository extends JpaRepository<EmailVerificationEntity, UUID> {

    Optional<EmailVerificationEntity> findByUserIdAndCode(UUID id, String code);

    List<EmailVerificationEntity> findByUserIdAndRevoked(UUID userId, boolean revoked, Sort sort);
}
