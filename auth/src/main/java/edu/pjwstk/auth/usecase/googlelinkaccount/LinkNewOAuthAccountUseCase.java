package edu.pjwstk.auth.usecase.googlelinkaccount;

import edu.pjwstk.auth.usecase.common.LoginUserResult;
import edu.pjwstk.core.UseCase;

import java.util.Optional;

public interface LinkNewOAuthAccountUseCase extends UseCase<LinkNewOAuthAccountCommand, Optional<LoginUserResult>> {
}
