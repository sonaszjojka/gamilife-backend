package edu.pjwstk.auth.api;

import edu.pjwstk.auth.usecase.GetAuthenticatedUserDataUseCase;
import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@AllArgsConstructor
public class AuthApiImpl implements AuthApi {

    private GetAuthenticatedUserDataUseCase getAuthenticatedUserDataUseCase;

    @Override
    public Optional<CurrentUserDto> getCurrentUser() {
        return getAuthenticatedUserDataUseCase.execute();
    }
}
