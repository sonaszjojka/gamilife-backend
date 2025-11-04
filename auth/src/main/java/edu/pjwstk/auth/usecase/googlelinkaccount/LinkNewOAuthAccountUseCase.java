package edu.pjwstk.auth.usecase.googlelinkaccount;

import edu.pjwstk.auth.usecase.login.LoginUserResult;

import java.util.Optional;

public interface LinkNewOAuthAccountUseCase {
    Optional<LoginUserResult> execute(LinkNewOAuthAccountCommand linkNewOAuthAccountCommand);
}
