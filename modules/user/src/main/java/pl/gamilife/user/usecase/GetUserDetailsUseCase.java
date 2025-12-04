package pl.gamilife.user.usecase;

import pl.gamilife.user.dto.service.UserDetailsDto;

import java.util.UUID;

public interface GetUserDetailsUseCase {
    UserDetailsDto execute(UUID userId);
}
