package pl.gamilife.auth.domain.port.repository;

import org.springframework.data.repository.query.Param;
import pl.gamilife.auth.domain.model.ForgotPasswordCode;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ForgotPasswordCodeRepository {

    Optional<ForgotPasswordCode> findByCodeAndRevokedAndExpiresAtIsGreaterThan(String code, boolean revoked, LocalDateTime expiresAtIsGreaterThan);

    List<ForgotPasswordCode> findNewestByUserIdAndRevoked(UUID userId, boolean revoked);

    void revokeAllActiveForgotPasswordCodesByUserId(@Param("userId") UUID userId);

    void save(ForgotPasswordCode forgotPasswordCode);
}
