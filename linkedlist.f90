program doublylinkedlist
    use class_DoublyList
    use class_DoublyLink
    implicit none
    
    integer :: i=0
    type(DoublyList) :: mylist
    class(DoublyLink),pointer :: doublyLinkItem

    type(DoublyListIterator),pointer :: itr
    
    !���X�g�ւ̐����C���������C�����̒ǉ��ƕ\��
    !print()�ŕ\���ł���̂͂���3��ނ̌^�̂�
    do i=1,10
        call mylist%add(i)
    end do
    call mylist%add(1.1)
    call mylist%add('A')
    call mylist%add('B')
    call mylist%add('C')
    call mylist%printList()
    print *,mylist%size()
    print *,"----------------"
    
    !���X�g�̃A�C�e����S�ď��������Ēǉ����s���e�X�g
    !call mylist%clear()
    !call mylist%addFirst(0)
    !call mylist%addLast("Z")
    !call mylist%printList()
    !print *,mylist%size()
    !print *,"----------------"
    
    
    !�v���O�����������鏈���̃p�^�[��
    !���X�g�̃A�C�e����1�̂Ƃ���remove����ƃA�N�Z�X�ᔽ������
    !call mylist%clear()
    !call mylist%add(0)
    !print *,mylist%size()
    !!call mylist%removeFirst() !�ǂ���g���Ă��A�N�Z�X�ᔽ
    !!call mylist%remove()      !
    !!call mylist%removeLast()  !
    !call mylist%printList()
    !print *,mylist%size()
    !print *,"----------------"

    !Iterator�̃e�X�g
    itr => mylist%iterator()
    call itr%first()
    do while(itr%hasNext())
        doublyLinkItem => toDoublyLink(itr%next())
        call doublyLinkItem%print()
    end do
    print *,"---"
    
    !first()��iterator�����X�g�̐擪�ɖ߂邩���e�X�g
    call itr%first()
    do while(itr%hasNext())
        doublyLinkItem => toDoublyLink(itr%next())
        call doublyLinkItem%print()
    end do
    
    call mylist%destroyIterator(itr)
    call mylist%clear()

end program doublylinkedlist