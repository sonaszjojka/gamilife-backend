package pl.gamilife.auth.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.api.auth.dto.ChangePasswordDto;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.auth.dto.RotateUserTokensDto;
import pl.gamilife.auth.application.changepassword.ChangePasswordCommand;
import pl.gamilife.auth.application.changepassword.ChangePasswordUseCase;
import pl.gamilife.auth.application.getauthuser.GetAuthenticatedUserCommand;
import pl.gamilife.auth.application.getauthuser.GetAuthenticatedUserDataUseCase;
import pl.gamilife.auth.application.rotatetokens.RotateUserTokensCommand;
import pl.gamilife.auth.application.rotatetokens.RotateUserTokensUseCase;

@Service
@AllArgsConstructor
public class AuthApiImpl implements AuthApi {

    private GetAuthenticatedUserDataUseCase getAuthenticatedUserDataUseCase;
    private ChangePasswordUseCase changePasswordUseCase;
    private RotateUserTokensUseCase rotateUserTokensUseCase;

    @Override
    public CurrentUserDto getCurrentUser() {
        return getAuthenticatedUserDataUseCase.execute(new GetAuthenticatedUserCommand());
    }

    @Override
    public String handleChangePassword(ChangePasswordDto dto) {
        return changePasswordUseCase.execute(ChangePasswordCommand.from(dto));
    }

    @Override
    public AuthTokens rotateUserTokens(RotateUserTokensDto dto) {
        return rotateUserTokensUseCase.execute(RotateUserTokensCommand.from(dto));
    }

}
