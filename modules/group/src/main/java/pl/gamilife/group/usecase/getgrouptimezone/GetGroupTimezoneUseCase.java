package pl.gamilife.group.usecase.getgrouptimezone;

import pl.gamilife.shared.kernel.architecture.UseCase;

import java.time.ZoneId;

public interface GetGroupTimezoneUseCase extends UseCase<GetGroupTimezoneCommand, ZoneId> {
}
