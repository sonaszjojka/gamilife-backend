package pl.gamilife.group.usecase.findgroupmemberbyuserid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;

import java.util.Optional;

@Service
@AllArgsConstructor
public class FindGroupMemberByUserIdUseCaseImpl implements FindGroupMemberByUserIdUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupJpaRepository groupRepository;

    @Override
    public Optional<FindGroupMemberByUserIdResult> execute(FindGroupMemberByUserIdCommand cmd) {
        Group group = groupRepository.findById(cmd.groupId()).orElseThrow(
                () -> new GroupNotFoundException("Group with id: " + cmd.groupId() + " not found")
        );

        var groupMember = groupMemberRepository.findWithGroupByUserIdAndGroupId(cmd.userId(), cmd.groupId());

        return groupMember.map(gm -> new FindGroupMemberByUserIdResult(
                gm.getId(),
                gm.getUserId(),
                gm.getGroupId(),
                group.getName(),
                gm.getGroupMoney(),
                gm.getTotalEarnedMoney(),
                gm.getLeftAt(),
                gm.getJoinedAt(),
                gm.isAdmin()
        ));
    }
}
