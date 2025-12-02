package edu.pjwstk.auth.api;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.api.auth.dto.ChangePasswordDto;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.auth.dto.RotateUserTokensDto;
import edu.pjwstk.auth.usecase.changepassword.ChangePasswordCommand;
import edu.pjwstk.auth.usecase.changepassword.ChangePasswordUseCase;
import edu.pjwstk.auth.usecase.getauthuser.GetAuthenticatedUserCommand;
import edu.pjwstk.auth.usecase.getauthuser.GetAuthenticatedUserDataUseCase;
import edu.pjwstk.auth.usecase.rotatetokens.RotateUserTokensCommand;
import edu.pjwstk.auth.usecase.rotatetokens.RotateUserTokensUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

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
