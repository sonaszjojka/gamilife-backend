package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.groups.entity.Group;

public interface FindGroupByIdMapper {
    GroupDto toResponse(Group savedGroup);
}
