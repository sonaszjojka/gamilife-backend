package edu.pjwstk.auth.persistence.repository.jpa;

import edu.pjwstk.auth.persistence.entity.ForgotPasswordCodeEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface JpaForgotPasswordCodeRepository extends JpaRepository<ForgotPasswordCodeEntity, UUID> {
}