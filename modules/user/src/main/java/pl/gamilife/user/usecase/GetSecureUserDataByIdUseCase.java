package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.SecureUserInfoDto;

import java.util.Optional;
import java.util.UUID;

public interface GetSecureUserDataByIdUseCase {
    Optional<SecureUserInfoDto> execute(UUID userId);
}
