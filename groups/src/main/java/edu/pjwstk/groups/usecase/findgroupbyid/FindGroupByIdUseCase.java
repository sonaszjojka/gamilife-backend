package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.api.groups.dto.GroupDto;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface FindGroupByIdUseCase extends UseCase<FindGroupByIdCommand, GroupDto> {
}
