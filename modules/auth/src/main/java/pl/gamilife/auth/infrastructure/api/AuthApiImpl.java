package pl.gamilife.auth.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.auth.application.usecase.getauthuser.GetAuthenticatedUserCommand;
import pl.gamilife.auth.application.usecase.getauthuser.GetAuthenticatedUserDataUseCase;

@Service
@AllArgsConstructor
public class AuthApiImpl implements AuthApi {

    private GetAuthenticatedUserDataUseCase getAuthenticatedUserDataUseCase;

    @Override
    public CurrentUserDto getCurrentUser() {
        return getAuthenticatedUserDataUseCase.execute(new GetAuthenticatedUserCommand());
    }

}
