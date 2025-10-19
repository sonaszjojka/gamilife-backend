package edu.pjwstk.groups.usecase.editgroup;

import edu.pjwstk.groups.entity.Group;
import org.springframework.stereotype.Component;

@Component
public class UpdateGroupMapperImpl implements UpdateGroupMapper {

    @Override
    public UpdateGroupResponse toResponse(Group savedGroup) {
        UpdateGroupResponse.GroupTypeDto groupTypeDto = null;
        if (savedGroup.getGroupType() != null) {
            groupTypeDto = new UpdateGroupResponse.GroupTypeDto(
                    savedGroup.getGroupType().getGroupTypeId(),
                    savedGroup.getGroupType().getTitle()
            );
        }

        return UpdateGroupResponse.builder()
                .groupId(savedGroup.getGroupId())
                .joinCode(savedGroup.getJoinCode())
                .adminId(savedGroup.getAdminId())
                .groupCurrencySymbol(savedGroup.getGroupCurrencySymbol())
                .membersLimit(savedGroup.getMembersLimit())
                .groupType(groupTypeDto)
                .build();
    }
}
