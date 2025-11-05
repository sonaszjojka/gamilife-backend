package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.groups.entity.Group;
import org.springframework.stereotype.Component;

@Component
public class FindGroupByIdMapperImpl implements FindGroupByIdMapper {

    @Override
    public GroupDto toResponse(Group savedGroup) {
        return GroupDto.builder()
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
