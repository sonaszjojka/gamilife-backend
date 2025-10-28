package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.groups.entity.Group;
import org.springframework.stereotype.Component;
import edu.pjwstk.common.groupsApi.dto.GroupDto;

@Component
public class FindGroupByIdMapperImpl implements FindGroupByIdMapper {

    @Override
    public GroupDto toResponse(Group savedGroup) {
        return edu.pjwstk.common.groupsApi.dto.GroupDto.builder()
                .groupId(savedGroup.getGroupId())
                .joinCode(savedGroup.getJoinCode())
                .adminId(savedGroup.getAdminId())
                .groupCurrencySymbol(savedGroup.getGroupCurrencySymbol())
                .membersLimit(savedGroup.getMembersLimit())
                .groupType(GroupDto.GroupTypeDto.builder()
                        .groupTypeId(savedGroup.getGroupType().getGroupTypeId())
                        .title(savedGroup.getGroupType().getTitle())
                        .build())
                .build();
    }
}
