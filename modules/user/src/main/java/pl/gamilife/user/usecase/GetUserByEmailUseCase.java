package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.BasicUserInfoApiDto;

import java.util.Optional;

public interface GetUserByEmailUseCase {
    Optional<BasicUserInfoApiDto> execute(String email);
}
