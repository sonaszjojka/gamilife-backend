package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.BasicUserInfoDto;

import java.util.Optional;

public interface GetUserByEmailUseCase {
    Optional<BasicUserInfoDto> execute(String email);
}
