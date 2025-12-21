package pl.gamilife.auth.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;
import pl.gamilife.auth.domain.model.ForgotPasswordCode;
import pl.gamilife.auth.domain.port.repository.ForgotPasswordCodeRepository;
import pl.gamilife.auth.infrastructure.persistence.jpa.JpaForgotPasswordCodeRepository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class ForgotPasswordCodeRepositoryAdapter implements ForgotPasswordCodeRepository {

    private final JpaForgotPasswordCodeRepository jpaForgotPasswordCodeRepository;

    @Override
    public Optional<ForgotPasswordCode> findByCodeAndRevokedAndExpiresAtIsGreaterThan(String code, boolean revoked, LocalDateTime expiresAtIsGreaterThan) {
        return jpaForgotPasswordCodeRepository.findByCodeAndRevokedAndExpiresAtIsGreaterThan(
                code,
                revoked,
                expiresAtIsGreaterThan
        );
    }

    @Override
    public List<ForgotPasswordCode> findNewestByUserIdAndRevoked(UUID userId, boolean revoked) {
        return jpaForgotPasswordCodeRepository.findByUserIdAndRevoked(
                userId,
                revoked,
                Sort.by(Sort.Direction.DESC, "issuedAt")
        );
    }

    @Override
    public void revokeAllActiveForgotPasswordCodesByUserId(UUID userId) {
        jpaForgotPasswordCodeRepository.revokeAllActiveForgotPasswordCodesByUserId(userId);
    }

    @Override
    public void save(ForgotPasswordCode forgotPasswordCode) {
        jpaForgotPasswordCodeRepository.save(forgotPasswordCode);
    }
}
