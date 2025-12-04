package pl.gamilife.auth.usecase.googlelinkaccount;

import pl.gamilife.auth.usecase.common.LoginUserResult;
import pl.gamilife.infrastructure.core.architecture.UseCase;

import java.util.Optional;

public interface LinkGoogleAccountUseCase extends UseCase<LinkGoogleAccountCommand, Optional<LoginUserResult>> {
}
