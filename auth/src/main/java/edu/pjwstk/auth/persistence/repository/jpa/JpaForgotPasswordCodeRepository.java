package edu.pjwstk.auth.persistence.repository.jpa;

import edu.pjwstk.auth.persistence.entity.ForgotPasswordCodeEntity;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface JpaForgotPasswordCodeRepository extends JpaRepository<ForgotPasswordCodeEntity, UUID> {
    Optional<ForgotPasswordCodeEntity> findByUserIdAndCode(UUID id, String code);

    List<ForgotPasswordCodeEntity> findByUserIdAndRevoked(UUID userId, boolean revoked, Sort sort);
}