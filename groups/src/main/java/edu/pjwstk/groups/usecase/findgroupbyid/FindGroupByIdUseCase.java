package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.api.groups.dto.GroupDto;

import java.util.UUID;

public interface FindGroupByIdUseCase {
    GroupDto execute(UUID groupId);
}
