package pl.gamilife.gamification.application.usecase.getallitemslots;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.port.repository.ItemSlotRepository;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetAllItemSlotsUseCaseImpl implements GetAllItemSlotsUseCase {

    private final ItemSlotRepository itemSlotRepository;

    @Override
    public GetAllItemSlotsResult execute(GetAllItemSlotsCommand cmd) {
        return new GetAllItemSlotsResult(
                itemSlotRepository.findAll().stream()
                        .map(itemSlot -> new GetAllItemSlotsResult.ItemSlotDTO(
                                itemSlot.getId(),
                                itemSlot.getName()
                        ))
                        .toList()
        );
    }
}
