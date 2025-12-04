package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.SecureUserInfoApiDto;

import java.util.Optional;
import java.util.UUID;

public interface GetSecureUserDataByIdUseCase {
    Optional<SecureUserInfoApiDto> execute(UUID userId);
}
