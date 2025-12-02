package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.SecureUserInfoApiDto;

import java.util.Optional;

public interface GetSecureUserDataByEmailUseCase {
    Optional<SecureUserInfoApiDto> execute(String email);
}
