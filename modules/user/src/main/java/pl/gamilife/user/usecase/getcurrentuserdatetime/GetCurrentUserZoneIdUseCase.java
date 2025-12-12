package pl.gamilife.user.usecase.getcurrentuserdatetime;

import pl.gamilife.shared.kernel.architecture.UseCase;

import java.time.ZoneId;

public interface GetCurrentUserZoneIdUseCase extends UseCase<GetCurrentUserZoneIdCommand, ZoneId> {
}
