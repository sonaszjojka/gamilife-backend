package pl.gamilife.group.usecase.findgroupbyid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;

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
                .groupId(group.getId())
                .groupName(group.getName())
                .joinCode(group.getJoinCode())
                .adminId(group.getAdminId())
                .groupCurrencySymbol(group.getCurrencySymbol())
                .membersLimit(group.getMembersLimit())
                .groupType(GroupDto.GroupTypeDto.builder()
                        .groupTypeId(group.getType().getId())
                        .title(group.getType().getTitle())
                        .build())
                .build();
    }
}
