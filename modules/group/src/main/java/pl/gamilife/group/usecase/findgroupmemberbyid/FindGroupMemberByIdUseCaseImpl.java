package pl.gamilife.group.usecase.findgroupmemberbyid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.group.dto.GroupMemberDto;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;

@Service
@AllArgsConstructor
public class FindGroupMemberByIdUseCaseImpl implements FindGroupMemberByIdUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    public GroupMemberDto execute(FindGroupMemberByIdCommand cmd) {
        Group group = groupRepository.findById(cmd.groupId()).orElseThrow(
                () -> new GroupNotFoundException("Group with id: " + cmd.groupId() + " not found")
        );

        GroupMember groupMember = groupMemberRepository.findById(cmd.groupMemberId())
                .orElseThrow(() -> new GroupMemberNotFoundException("Group member with id: " +
                        cmd.groupMemberId() + " not found")
                );
        return buildGroupMemberDto(group, groupMember);
    }

    private GroupMemberDto buildGroupMemberDto(Group group, GroupMember groupMember) {
        return GroupMemberDto.builder()
                .groupMemberId(groupMember.getId())
                .memberGroup(new GroupMemberDto.GroupDto(group.getId(), group.getName()))
                .userId(groupMember.getUserId())
                .joinedAt(groupMember.getJoinedAt())
                .leftAt(groupMember.getLeftAt())
                .groupMoney(groupMember.getGroupMoney())
                .totalEarnedMoney(groupMember.getTotalEarnedMoney())
                .build();
    }
}
