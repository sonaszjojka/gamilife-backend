package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.usecase.command.LinkNewOAuthAccountCommand;
import edu.pjwstk.auth.usecase.result.LoginUserResult;

import java.util.Optional;

public interface LinkNewOAuthAccountUseCase {
    Optional<LoginUserResult> execute(LinkNewOAuthAccountCommand linkNewOAuthAccountCommand);
}
