package pl.gamilife.group.usecase.findgroupmemberbyuserid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.group.repository.GroupMemberJpaRepository;

import java.util.Optional;

@Service
@AllArgsConstructor
public class FindGroupMemberByUserIdUseCaseImpl implements FindGroupMemberByUserIdUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    public Optional<FindGroupMemberByUserIdResult> execute(FindGroupMemberByUserIdCommand cmd) {
        var groupMember = groupMemberRepository.findByUserIdAndGroupId(cmd.userId(), cmd.groupId());

        return groupMember.map(gm -> new FindGroupMemberByUserIdResult(
                gm.getId(),
                gm.getUserId(),
                gm.getGroupId(),
                gm.getGroupMoney(),
                gm.getTotalEarnedMoney(),
                gm.getLeftAt(),
                gm.getJoinedAt()
        ));
    }
}
