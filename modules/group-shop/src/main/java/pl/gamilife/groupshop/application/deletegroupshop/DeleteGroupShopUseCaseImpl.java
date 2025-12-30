package pl.gamilife.groupshop.application.deletegroupshop;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;

@Service
@AllArgsConstructor
@Transactional
public class DeleteGroupShopUseCaseImpl implements DeleteGroupShopUseCase {

    private final GroupShopRepository groupShopRepository;

    @Override
    public Void execute(DeleteGroupShopCommand cmd) {
        groupShopRepository.deleteByGroupId(cmd.groupId());

        return null;
    }
}
