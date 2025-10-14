package edu.pjwstk.groups.usecase.creategroup;

import edu.pjwstk.groups.domain.Group;
import edu.pjwstk.groups.domain.GroupType;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class CreateGroupMapperImpl implements CreateGroupMapper {

    @Override
    public Group toEntity(CreateGroupRequest request, String joinCode, UUID uuid, GroupType groupType) {

        return Group.builder()
                .groupId(uuid)
                .joinCode(joinCode)
                .adminId(request.adminId())
                .groupCurrencySymbol(request.groupCurrencySymbol())
                .membersLimit(request.membersLimit())
                .groupType(groupType)
                .build();
    }

    @Override
    public CreateGroupResponse toResponse(Group savedGroup) {
        CreateGroupResponse.GroupTypeDto groupTypeDto = null;
        if (savedGroup.getGroupType() != null) {
            groupTypeDto = new CreateGroupResponse.GroupTypeDto(
                    savedGroup.getGroupType().getGroupTypeId(),
                    savedGroup.getGroupType().getTitle()
            );
        }

        return CreateGroupResponse.builder()
                .groupId(savedGroup.getGroupId())
                .joinCode(savedGroup.getJoinCode())
                .adminId(savedGroup.getAdminId())
                .groupCurrencySymbol(savedGroup.getGroupCurrencySymbol())
                .membersLimit(savedGroup.getMembersLimit())
                .groupType(groupTypeDto)
                .build();
    }

}
