package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.api.groups.dto.GroupDto;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class FindGroupByIdUseCaseImpl implements FindGroupByIdUseCase {

    private final GroupJpaRepository groupRepository;

    @Override
    public GroupDto execute(FindGroupByIdCommand cmd) {
        Group group = groupRepository.findById(cmd.groupId())
                .orElseThrow(() -> new GroupNotFoundException("Group with id:" + cmd.groupId() + " not found!"));

        return buildGroupDto(group);
    }

    private GroupDto buildGroupDto(Group group) {
        return GroupDto.builder()
                .groupId(group.getGroupId())
                .groupName(group.getName())
                .joinCode(group.getJoinCode())
                .adminId(group.getAdminId())
                .groupCurrencySymbol(group.getGroupCurrencySymbol())
                .membersLimit(group.getMembersLimit())
                .groupType(GroupDto.GroupTypeDto.builder()
                        .groupTypeId(group.getGroupType().getGroupTypeId())
                        .title(group.getGroupType().getTitle())
                        .build())
                .build();
    }
}
