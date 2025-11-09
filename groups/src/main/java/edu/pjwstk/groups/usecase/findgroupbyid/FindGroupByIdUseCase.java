package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.core.UseCase;

import java.util.UUID;

public interface FindGroupByIdUseCase extends UseCase<FindGroupByIdCommand, GroupDto> {
}
