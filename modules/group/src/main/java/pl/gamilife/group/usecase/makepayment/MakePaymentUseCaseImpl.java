package pl.gamilife.group.usecase.makepayment;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;

@Service
@Transactional
@AllArgsConstructor
public class MakePaymentUseCaseImpl implements MakePaymentUseCase {

    private final GroupMemberJpaRepository groupMemberRepository;

    @Override
    public Void execute(MakePaymentCommand cmd) {
        GroupMember member = groupMemberRepository.findById(cmd.groupMemberId()).orElseThrow(
                () -> new GroupMemberNotFoundException("Group Member with Id " + cmd.groupMemberId() + " does not exist")
        );

        member.payMoney(cmd.amount());

        groupMemberRepository.save(member);

        return null;
    }
}
