package pl.gamilife.group.usecase.findgroupbyid;

import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface FindGroupByIdUseCase extends UseCase<FindGroupByIdCommand, GroupDto> {
}
