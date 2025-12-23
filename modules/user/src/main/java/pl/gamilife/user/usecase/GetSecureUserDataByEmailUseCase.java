package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.SecureUserInfoDto;

import java.util.Optional;

public interface GetSecureUserDataByEmailUseCase {
    Optional<SecureUserInfoDto> execute(String email);
}
