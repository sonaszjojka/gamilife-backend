package pl.gamilife.auth.application.usecase.googlesignin;

import pl.gamilife.auth.application.dto.GoogleSignInResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface GoogleSignInUseCase extends UseCase<GoogleSignInCommand, GoogleSignInResult> {
}
